FROM ubuntu
RUN apt-get update && apt-get install -q -y libpq-dev wget libicu-dev
RUN wget -P /bin https://godist.herokuapp.com/projects/ddollar/forego/releases/current/linux-amd64/forego
RUN chmod u+x /bin/forego
ADD . /run
WORKDIR /run