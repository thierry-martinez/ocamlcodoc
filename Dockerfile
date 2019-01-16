FROM debian:testing
RUN apt-get update && apt-get -y install opam
RUN useradd -m -s /bin/bash ci
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
RUN opam init --auto-setup --compiler 4.07.1