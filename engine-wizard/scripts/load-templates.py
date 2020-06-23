import click
import json
import mimetypes
import logging
import os
import requests
import sys

TEMPLATE_FILENAME = 'template.json'


def walk_files(root_dir):
    for root, dirs, files in os.walk(root_dir):
        for file in files:
            yield root, file, os.path.join(root, file)


def extract_allowed_packages(data):
    allowed_packages = [
        {
            'orgId': ap.get('orgId', None),
            'kmId': ap.get('kmId', None),
            'minVersion': ap.get('minVersion', None),
            'maxVersion': ap.get('maxVersion', None),
        } for ap in data.get('allowedPackages', [])
    ]
    return allowed_packages


def extract_steps(data):
    steps = [
        {
            'name': step['name'],
            'options': step.get('options', {}),
        } for step in data.get('steps', [])
    ]
    return steps


def extract_formats(data):
    formats = [
        {
            'uuid': f['uuid'],
            'name': f['name'],
            'shortName': f['shortName'],
            'icon': f['icon'],
            'color': f['color'],
            'steps': extract_steps(f),
        } for f in data.get('formats', [])
    ]
    return formats


def validate_template(template_file, template_data):
    mandatory = ['name', 'id', 'organizationId', 'templateId']
    missing = [key for key in mandatory if key not in template_data.keys()]
    if len(missing) == 0:
        return True
    x = ', '.join(missing)
    logging.error(f'Missing template attributes {x} in {template_file}')


def extract_template(template_file):
    with open(template_file, mode='r') as f:
        data = json.load(f)
    if not validate_template(template_file, data):
        return None
    name = data['name']
    template = {
        'name': data['name'],
        'id': data['id'],
        'organizationId': data['organizationId'],
        'templateId': data['templateId'],
        'metamodelVersion': 1,
        'readme': data.get('readme', f'# {name}'),
        'description': data.get('description', ''),
        'version': data.get('version', '0.1.0'),
        'allowedPackages': extract_allowed_packages(data),
        'recommendedPackageId': data.get('recommendedPackageId', None),
        'license': data.get('license', 'nolicense'),
        'formats': extract_formats(data)
    }
    return template


def is_text_file(filename: str):
    return filename.endswith('.j2') or mimetypes.guess_type(filename)[0].startswith('text')


def extract_files(template_dir):
    files = []
    for root, file, filepath in walk_files(template_dir):
        if file != TEMPLATE_FILENAME and is_text_file(file):
            rel_path = os.path.relpath(filepath, template_dir)
            with open(filepath, mode='r', encoding='utf-8') as f:
                content = f.read()
            files.append({
                'fileName': rel_path,
                'content': content
            })
    return files


def extract_assets(template_dir):
    assets = []
    for root, file, filepath in walk_files(template_dir):
        if file != TEMPLATE_FILENAME and not is_text_file(file):
            rel_path = os.path.relpath(filepath, template_dir)
            content_type = mimetypes.guess_type(file)[0]
            if content_type is None:
                content_type = 'application/octet-stream'
            assets.append((rel_path, filepath, content_type))
    return assets


def process_template(template_file, comm, db_template_ids):
    logging.info(f'Processing {template_file}')
    template_dir = os.path.dirname(template_file)
    template_object = extract_template(template_file)
    if not template_object:
        logging.info(f'Skipping {template_file}')
        return
    files = extract_files(template_dir)
    assets = extract_assets(template_dir)

    if template_object['id'] in db_template_ids:
        logging.info(f'- DELETE template (existing)')
        comm.delete_template(template_object['id'])
    logging.info(f'- POST template')
    template = comm.post_template(template_object)
    template_id = template['id']
    logging.info(f'- POST files ({len(files)} in total)')
    for file in files:
        comm.post_template_file(template_id, file)
    logging.info(f'- POST assets ({len(assets)} in total)')
    for asset in assets:
        comm.post_template_asset(template_id, *asset)
    logging.info(f'- Processing {template_file} ({template_id})')


def template_files(root_dir):
    return (fp for r, f, fp in walk_files(root_dir)
            if f == TEMPLATE_FILENAME)


class DSWCommunicator:

    def __init__(self, api_url, service_token, session=None):
        self.api_url = api_url
        self.service_token = service_token
        self.session = session or requests.Session()
        self.session.auth = self._session_auth()

    def _session_auth(self):
        def dsw_auth(req):
            req.headers.update({
                'Authorization': 'Bearer ' + self.service_token,
            })
            return req
        return dsw_auth

    def get_templates(self):
        resp = self.session.get(
            url=f'{self.api_url}/templates'
        )
        resp.raise_for_status()
        return resp.json()

    def delete_template(self, template_id):
        self.session.delete(
            url=f'{self.api_url}/templates/{template_id}'
        )

    def post_template(self, template):
        resp = self.session.post(
            url=f'{self.api_url}/templates',
            json=template,
            headers={
                'Content-Type': 'application/json',
            }
        )
        resp.raise_for_status()
        return resp.json()

    def post_template_file(self, template_id, file):
        resp = self.session.post(
            url=f'{self.api_url}/templates/{template_id}/files',
            json=file,
            headers={
                'Content-Type': 'application/json',
            }
        )
        resp.raise_for_status()
        return resp.json()

    def post_template_asset(self, template_id, name, filepath, content_type):
        resp = self.session.post(
            url=f'{self.api_url}/templates/{template_id}/assets',
            files={'file': (name, open(filepath, mode='rb'), content_type)},
        )
        resp.raise_for_status()
        return resp.json()


@click.command()
@click.argument("templates_root", envvar='TEMPLATES_ROOT',
                type=click.Path(exists=True, file_okay=False, dir_okay=True))
@click.argument("api_url", envvar='API_URL', type=str)
@click.argument("service_token", envvar='SERVICE_TOKEN', type=str)
def cli(templates_root, api_url, service_token):
    logging.basicConfig(
        stream=sys.stdout,
        level=logging.INFO,
        format='%(asctime)s | %(levelname)s | %(module)s: %(message)s'
    )
    comm = DSWCommunicator(api_url, service_token)
    db_templates = comm.get_templates()
    db_template_ids = set([template['id'] for template in db_templates])
    for template_file in template_files(templates_root):
        process_template(template_file, comm, db_template_ids)


if __name__ == '__main__':
    cli()
