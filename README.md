# Dockerized Shiny Application 

The provided Dockerfile serves as a template for a Docker image that enables to host a Shiny application via Shiny-server. \
All configuration files can be found in the **Shiny-Server** folder. To build a Docker running your Shiny application, copy its code to the **Shiny-server** folder as well.

## Installation

To build a new Docker image run the following command within the **Shiny-Server** folder 

```bash
> docker build -t qcshiny_image -f Dockerfile.txt .
```

To start a Docker container exposed on port 3838 run

```bash
> docker run -p 3838:3838 -d --name qcshiny_container qcshiny_image
```



## Usage

Once the container runs, the web application is available at http://localhost:3838/

If the application aborts, logs can be accessed by copying the logs file to the host system by running 
```bash
> docker cp qcshiny_container:/var/log/shiny-server ./logs
````
