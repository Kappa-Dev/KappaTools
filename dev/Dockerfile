FROM quay.io/pypa/manylinux2010_x86_64

RUN yum install -y rsync

RUN adduser -p '' opam && \
  passwd -l opam && \
  chown -R opam:opam /home/opam

RUN curl -L https://github.com/ocaml/opam/archive/2.0.4.tar.gz | tar xz && \
  cd opam-2.0.4 && make cold && make cold-install && cd .. && rm -rf opam-2.0.4

USER opam
ENV HOME /home/opam
WORKDIR /home/opam

RUN opam init -a -y --disable-sandboxing
COPY build_wheel.sh ./

ENTRYPOINT [ "opam", "config", "exec", "--" ]
