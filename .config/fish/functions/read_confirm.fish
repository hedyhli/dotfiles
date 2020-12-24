function read_confirm
  while true
    read -l -P "$argv[1] [y/N] " confirm

    switch $confirm
      case Y y
        return 0
      case '' N n
        return 1
    end
  end
end

