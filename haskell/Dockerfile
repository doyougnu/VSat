# Build the haskell binary
FROM haskell:8.2.2
WORKDIR /vsat
ADD . /vsat
RUN stack setup --no-nix
RUN stack build --no-nix --copy-bins

# Build the most recent z3 (needed to comply with SBV)
RUN apt-get update && apt-get install -y \
    git \
    python3 \
    build-essential
COPY buildScripts/buildz3.sh .
RUN chmod +x buildz3.sh
RUN ./buildz3.sh

# Set the image to call the binary and expose a port
EXPOSE 80
CMD [ "vsat" ]