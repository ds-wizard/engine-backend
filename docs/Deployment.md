# Deployment

The application is distributed in a form of a Docker image. So everyone with installed Docker can run the application.

## Building Image

We create our own Docker image which encapsulate the application (`dsw-server`). As a template we used our another image (`stack-hpack`). Our image contains both - source codes and compiled application. An advantage is that we do not have to have 2 images - one for build and one for running a compiled application. A disadvantage is a size of Docker image (it has to include source codes and build tools).

![Docker images](/docs/images/docker-images.png)

We build a Docker image automatically after a detected change in a master branch. A produced image is then uploaded to our Docker Registry.

## Running Containers

On server we set up Docker Compose which is a tool to simplify a deployment process of more Docker Containers. A principle of Docker Compose is to write one file where you describe which containers you want to start, how they are connected, which files you want to mount in, etc.

Here is an example of a configuration for Docker Compose.

```
version: '3'
services:

	mongo:
		image: mongo
		ports:
		- 27017:27017

	dsw_server:
		image: ccmi-elixir.cesnet.cz:5000/elixir/dsw-server
		ports:
			- 3000:443
		links:
			- mongo

	dsw_client:
		image: ccmi-elixir.cesnet.cz:5000/elixir/dsw-client
		environment:
			- API_URL=https://api.dsw.fairdata.solutions

	dsw_nginx:
		image: nginx
		ports:
			- 80:80
			- 443:443
		volumes:
			- ....
		links:
			- dsw_client
			- dsw_server
```

We can see from the example that we has to deploy also Nginx. Its role here is to serve as a reverse proxy and holds certificate for a client and a server so they do not have to manage certificates by themselves and offer secure connection.

For better understanding how the containers are connected now, we created a deployment diagram (see below).

![Deplyment Diagram (Production Server)](/docs/images/deployment-diagram.png)

The application is currently deployed on a server provided by FIT CTU. Here are the addresses of running applications:

- **Server:** https://api.dsw.fairdata.solutions
- **Client:** https://dsw.fairdata.solutions
