FROM gonz/quanterall-development:lts-18.14

RUN apt-get update && apt-get install -y \
    xvfb \
    fonts-liberation

# Download chrome stable version 95
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
    dpkg -i google-chrome-stable_current_amd64.deb && \
    rm google-chrome-stable_current_amd64.deb