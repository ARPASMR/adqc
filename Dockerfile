FROM arpasmr/r-base
COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts
RUN chmod a+x *.sh
CMD ["./launcher.sh"]
