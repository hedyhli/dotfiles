function apt-up
    sudo apt update && sudo apt upgrade -y
    sudo apt-get dist-upgrade -y
    sudo apt autoremove
end

