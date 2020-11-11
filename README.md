# Dockerized Shiny Application for Quality Control Reports

The provided Docker image enables to host a QC Shiny application based on the R package [PTXQC](https://github.com/cbielow/PTXQC) via Shiny-server. \
All configuration files as well as the Shiny application's code can be found in the **Shiny-Server** folder. 

## Installation

To build a new Docker image run the following command withing the **Shiny-Server** folder 

```bash
> docker build -t qcshiny_image -f Dockerfile.txt .
```

To start a Docker container exposed on port 3838 run

```bash
> docker run -p 3838:3838 -d --name qcshiny_container qcshiny_image
```



## Usage

Once the container runs, the web application is available in the browser at http://localhost:3838/

If the application aborts, logs can be accessed by copying the logs file to the host system by running 
```bash
> docker cp qcshiny_container:/var/log/shiny-server ./logs
````
