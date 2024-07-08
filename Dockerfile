# STAGE 0 - Ubuntu packages and R repository
FROM ubuntu as stage0
RUN echo "America/New_York" | tee /etc/timezone \
	&& apt update \
	&& DEBIAN_FRONTEND=noninteractive apt install -y \
		build-essential \
		gcc \
		gfortran \
        locales \
		libcurl4-gnutls-dev \
		libfontconfig1-dev \
		libfribidi-dev \
		libgit2-dev \
		libharfbuzz-dev \
		libnetcdf-dev \
		libnetcdff-dev \
		libssl-dev \
		libtiff5-dev \
		libxml2-dev \
		tzdata \
		wget \
    && locale-gen en_US.UTF-8

# STAGE 1 - R and R packages
FROM stage0 as stage1
RUN apt -y install \
		software-properties-common \
		dirmngr \
	&& . /etc/lsb-release \
	&& wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
	&& add-apt-repository -y "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" \ 
	&& apt update && apt -y install \
		r-base \
		r-base-dev \
	&& rm -rf /var/lib/apt/lists/* \
	&& /usr/bin/Rscript -e "install.packages('doParallel', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('foreach', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('hydroGOF', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('RNetCDF', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && /usr/bin/Rscript -e "install.packages('R.utils', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/')" 

# STAGE 2 set up I/O directories, copy geobamdata installer and R script
FROM stage1 as stage2
COPY ./mommadata/ /app/mommadata/

# STAGE 3 - Execute algorithm
FROM stage2 as stage3
LABEL version="1.0" \
	description="Containerized MOMMA algorithm." \
	"confluence.contact"="ntebaldi@umass.edu" \
	"algorithm.contact"="rwdudley@usgs.gov, dmbjerkl@usgs.gov"
ENTRYPOINT [ "/usr/bin/Rscript",  "/app/mommadata/momma_data.R" ]