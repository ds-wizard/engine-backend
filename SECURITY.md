# Security Policy

## Supported Versions

Always use the latest version of DSW (including patch version).
You can easily find the recommended (latest) versions in [DSW Deployment Example](https://github.com/ds-wizard/dsw-deployment-example) repository.
Older versions are not being patched as the upgrade process is simple and
described in our [User Guide](https://guide.ds-wizard.org/miscellaneous/self-hosted-dsw/upgrade-guidelines).

## Reporting a Vulnerability

In case you find a vulnerability, please report it via [issue](https://github.com/ds-wizard/ds-wizard/issues/new/choose)
or email as soon as possible.

You can directly propose a solution using a Pull Request (see [CONTRIBUTING](CONTRIBUTING.md) file).

More information on vulnerability reporting, known and solved vulnerabilities
is also available in our [User Guide](https://guide.ds-wizard.org/miscellaneous/self-hosted-dsw/upgrade-guidelines).

## Security Audit and Hotfixing

We use [Grype](https://github.com/anchore/grype) to daily check our latest releases of Docker images (directly in the [DSW Deployment Example](https://github.com/ds-wizard/dsw-deployment-example) repository). Once a critical vulnerability is found, we are notified and start working on a hotfix version.

You can check the scanning and results [here](https://github.com/ds-wizard/dsw-deployment-example/actions/workflows/security-audit.yml).