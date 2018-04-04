FROM quay.io/pypa/manylinux1_x86_64

RUN curl -L https://github.com/ocaml/opam/archive/2.0.0-rc.tar.gz | tar xz
RUN cd opam-2.0.0-rc && make cold && make cold-install
RUN rm -rf opam-2.0.0-rc

RUN yum install -y rsync

RUN adduser -p '' opam && \
  passwd -l opam && \
  chown -R opam:opam /home/opam
USER opam
ENV HOME /home/opam
WORKDIR /home/opam

RUN opam init -a -y
COPY build_wheel.sh ./

ENTRYPOINT [ "opam", "config", "exec", "--" ]
