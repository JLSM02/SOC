subroutine standardSimulation (l, limit, statistic, randomness, filename) !Open boundaries, does not check for area or lost sand
    implicit none

    ! Input arguments
    integer, intent(in) :: l, limit, statistic
    logical, intent(in) :: randomness
    character(len=6), intent(in) :: filename

    ! Internal Variables
    integer :: j, i, k, i_randy, i_randx, queue_length, nf_queue_length, time, size, duration, x, y, i_randn, frame_size
    real :: r_randy, r_randx, r_randn
    logical :: next_sand, topling
    integer :: M(l, l), Queue(2,l*l*limit*2), nf_Queue(2,l*l*limit*2)

    ! Variable inicialization
    M = 0 ! Matrix where the simulation will be done
    time = 0 ! Every frame of the simulation
    nf_Queue = 0 ! Starting Queue as 0
    !lost_sand = 0 ! Count lost sand in open boundaries
    

    !Formats
    20 format (1f8.4, a)
    30 format (I10,a,I10,a,I10)
    40 format (I10,a,I10)

    ! Files opening
    open (1, file = "temp/AdditionData"//filename//".dat", status = 'new')
    open (2, file = "temp/FrameData"//filename//".dat", status = 'new')

    ! Main loop (j counts the total sand added)
    MainLoop: do j = 1, (statistic*(l**2)) 
        
        size = 0 ! Number of toplings in a random addition process
        duration = 0 ! Amount of time needed in a random addition process
        next_sand = .false. ! Control var to exit the loop when te queue is empty
        topling = .false. ! Control var to to tell if the topling should be computed

        ! Generating random numbers for adding first sand
        call random_number(r_randx)
        call random_number(r_randy)
        i_randx = int(r_randx*l+1)
        i_randy = int(r_randy*l+1)

        ! Adding first random sand to the queue
        nf_Queue(1,1) = i_randx
        nf_Queue(2,1) = i_randy
        nf_queue_length = 1

        ! Loop for processing queue, every step is a frame
        EachFrameLoop: do 
            if (next_sand) EXIT
            ! Manage internal variables
            time = time + 1 ! Time increase each frame
            duration = duration + 1 
            frame_size = 0 ! Number of toplings in a frame
            Queue = nf_Queue
            queue_length = nf_queue_length
            nf_queue_length = 0

            InFrameLoop: do i = 1, queue_length ! Go through the previous frame queue
                ! Getting coords from queue
                x = Queue(1,i)
                y = Queue(2,i)
                ! Checking boundaries
                if (x > 0 .and. x < l+1 .and. y > 0 .and. y < l+1) then ! Normal case
                    M(x,y) = M(x,y) + 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if
                end if

                ! Topling
                if (topling) then
                    topling = .false.
                    M(x,y) = M(x,y) - limit
                    size = size + 1
                    frame_size = frame_size + 1

                    if (randomness) then ! Random distribution case
                    ! 1 2 3
                    ! 8 9 4
                    ! 7 6 5
                        do k = 1, limit
                            call random_number(r_randn)
                            i_randn = int(r_randn*9+1)
                            select case (i_randn)
                            case (1)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (2)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y-1

                            case (3)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (4)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y

                            case (5)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (6)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y+1

                            case (7)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (8)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y

                            case (9)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y

                            end select
                        end do

                    else ! Ordered distribution case
                        if (limit == 4) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y
                        
                        elseif (limit == 5) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y

                        elseif (limit == 8) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                        elseif (limit == 9) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y
                        end if
                    end if

                end if
            end do InFrameLoop
            if (nf_queue_length == 0) then
                next_sand = .true.
            end if

            ! Writing data of the frame
            write(2,40) time, ",", frame_size
        end do EachFrameLoop

        ! Printing the current completation percentage
        write (*,20) real(j)*100/real(statistic*l*l),"% done"

        ! Writing data of the addition
        write(1,30) time, ",", size, ",", duration

    end do MainLoop
    print *, "Simulation terminated"
    close(1)
    print *, "First temp file closed"
    close(2)
    print *, "Second temp file closed"

    print *, "Terminating subroutine"
end subroutine standardSimulation





subroutine completeSimulation (l, limit, tsa, statistic, bdt, randomness, filename)
    implicit none

    ! Input arguments
    integer, intent(in) :: l, limit, statistic, tsa !tsa (topling dand amount) tell us how much sand is redistributed im a topling
    logical, intent(in) :: randomness
    character(len=6), intent(in) :: bdt, filename

    ! Internal Variables
    integer :: j, i, k, i_randy, i_randx, queue_length, nf_queue_length, time, size, duration, x, y, i_randn, frame_size
    integer :: lost_sand, sand_in_model !Enable to check if sand-processes are missing
    integer :: area !Enable if want to compute the area
    real :: r_randy, r_randx, r_randn
    logical :: next_sand, topling
    integer :: M(l, l), Queue(2,l*l*limit*2), nf_Queue(2,l*l*limit*2)
    integer :: A(l,l) !Enable if want to compute the area

    ! Variable inicialization
    M = 0 ! Matrix where the simulation will be done
    time = 0 ! Every frame of the simulation
    nf_Queue = 0 ! Starting Queue as 0
    lost_sand = 0 ! Count lost sand in open boundaries
    

    !Formats
    20 format (1f8.4, a)
    30 format (I10,a,I10,a,I10,a,I10,a,I10)
    40 format (I10,a,I10,a,I10,a,I10)

    ! Files opening
    open (1, file = "temp/AdditionData"//filename//".dat", status = 'new')
    open (2, file = "temp/FrameData"//filename//".dat", status = 'new')

    ! Main loop (j counts the total sand added)
    MainLoop: do j = 1, (statistic*(l**2)) 

        ! Restoring variables to 0
        A = 0 ! Matrix for compute area
        size = 0 ! Number of toplings in a random addition process
        area = 0 ! Number cells affected in a random addition process
        duration = 0 ! Amount of time needed in a random addition process
        next_sand = .false. ! Control var to exit the loop when te queue is empty
        topling = .false. ! Control var to to tell if the topling should be computed

        ! Generating random numbers for adding first sand
        call random_number(r_randx)
        call random_number(r_randy)
        i_randx = int(r_randx*l+1)
        i_randy = int(r_randy*l+1)

        ! Adding first random sand to the queue
        nf_Queue(1,1) = i_randx
        nf_Queue(2,1) = i_randy
        nf_queue_length = 1

        ! Loop for processing queue, every step is a frame
        EachFrameLoop: do 
            if (next_sand) EXIT

            ! Manage internal variables
            time = time + 1 ! Time increase each frame
            duration = duration + 1 
            frame_size = 0 ! Number of toplings in a frame
            Queue = nf_Queue
            queue_length = nf_queue_length
            nf_queue_length = 0

            InFrameLoop: do i = 1, queue_length ! Go through the previous frame queue
                ! Getting coords from queue
                x = Queue(1,i)
                y = Queue(2,i)
                ! Checking boundaries
                if (x > 0 .and. x < l+1 .and. y > 0 .and. y < l+1) then ! Normal case
                    M(x,y) = M(x,y) + 1
                    A(x,y) = 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "closed") then ! Closed boundaries case
                    if (x < 1) then
                        x = 1
                    elseif (x > l) then
                        x = l
                    end if

                    if (y < 1) then
                        y = 1
                    elseif (y > l) then
                        y = l
                    end if

                    M(x,y) = M(x,y) + 1
                    A(x,y) = 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "ciclic") then ! Ciclic boundaries case
                    if (x < 1) then
                        x = l
                    elseif (x > l) then
                        x = 1
                    end if

                    if (y < 1) then
                        y = l
                    elseif (y > l) then
                        y = 1
                    end if

                    M(x,y) = M(x,y) + 1
                    A(x,y) = 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "open") then
                    lost_sand = lost_sand + 1
                
                else
                    print *, "Invalid boundary type"
                    exit MainLoop
                end if

                ! Topling
                if (topling) then
                    topling = .false.
                    M(x,y) = M(x,y) - tsa
                    size = size + 1
                    frame_size = frame_size + 1

                    if (randomness) then ! Random distribution case
                    ! 1 2 3
                    ! 8 9 4
                    ! 7 6 5
                        do k = 1, tsa
                            call random_number(r_randn)
                            i_randn = int(r_randn*9+1)
                            select case (i_randn)
                            case (1)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (2)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y-1

                            case (3)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (4)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y

                            case (5)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (6)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y+1

                            case (7)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (8)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y

                            case (9)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y

                            end select
                        end do

                    else ! Ordered distribution case
                        if (tsa == 4) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y
                        
                        elseif (tsa == 5) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y

                        elseif (tsa == 8) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                        elseif (tsa == 9) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y
                        end if
                    end if

                end if
            end do InFrameLoop
            if (nf_queue_length == 0) then
                next_sand = .true.
            end if

            sand_in_model = sum(M)
            area = sum(A)

            ! Writing data of the frame
            write(2,40) time, ",", frame_size, ",", lost_sand, ",", sand_in_model
        end do EachFrameLoop

        ! Printing the current completation percentage
        write (*,20) real(j)*100/real(statistic*l*l),"% done"

        ! Writing data of the addition
        write(1,30) time, ",", size, ",", duration, ",", area, ",", sand_in_model

    end do MainLoop

    close(1)
    close(2)

end subroutine completeSimulation





subroutine genAnimData (l, limit, frames, bdt, randomness)

    implicit none
    ! Input arguments
    integer, intent(in) :: l, limit, frames
    logical, intent(in) :: randomness
    character(len=6), intent(in) :: bdt

    ! Internal Variables
    integer :: j, i, k, i_randy, i_randx, queue_length, nf_queue_length, time, x, y, i_randn, frame_size
    real :: r_randy, r_randx, r_randn
    logical :: next_sand, topling
    integer :: M(l, l), Queue(2,l*l*limit*2), nf_Queue(2,l*l*limit*2), F(l,l), A(l, l)

    ! Variable inicialization
    M = 0 ! Matrix where the simulation will be done
    time = 0 ! Every frame of the simulation

    !Formats
    10 format (I3,a)
    20 format (I3)
    30 format (1f8.4, a)

    ! Files opening
    open (1, file = "temp/AnimDataM.dat", status = 'new')
    open (2, file = "temp/AnimDataF.dat", status = 'new')
    open (3, file = "temp/AnimDataA.dat", status = 'new')

    ! Main loop (j counts the total sand added)
    MainLoop: do j = 1, frames

        ! Restoring variables to 0
        A = 0 ! Matrix for telling the toplings fronts
        next_sand = .false. ! Control var to exit the loop when te queue is empty
        topling = .false. ! Control var to to tell if the topling should be computed

        ! Generating random numbers for adding first sand
        call random_number(r_randx)
        call random_number(r_randy)
        i_randx = int(r_randx*l+1)
        i_randy = int(r_randy*l+1)

        ! Adding first random sand to the queue
        nf_Queue(1,1) = i_randx
        nf_Queue(2,1) = i_randy
        nf_queue_length = 1

        ! Loop for processing queue, every step is a frame
        EachFrameLoop: do 
            if (next_sand) exit

            if (time > frames) exit MainLoop

            F = 0

            ! Manage internal variables
            Queue = nf_Queue
            queue_length = nf_queue_length
            nf_queue_length = 0

            InFrameLoop: do i = 1, queue_length ! Go through the previous frame queue

                ! Getting coords from queue
                x = Queue(1,i)
                y = Queue(2,i)
                ! Checking boundaries
                if (x > 0 .and. x < l+1 .and. y > 0 .and. y < l+1) then ! Normal case

                    M(x,y) = M(x,y) + 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "closed") then ! Closed boundaries case
                    if (x < 1) then
                        x = 1
                    elseif (x > l) then
                        x = l
                    end if

                    if (y < 1) then
                        y = 1
                    elseif (y > l) then
                        y = l
                    end if

                    M(x,y) = M(x,y) + 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "ciclic") then ! Ciclic boundaries case
                    if (x < 1) then
                        x = l
                    elseif (x > l) then
                        x = 1
                    end if

                    if (y < 1) then
                        y = l
                    elseif (y > l) then
                        y = 1
                    end if

                    M(x,y) = M(x,y) + 1
                    if (M(x,y) == limit) then
                        topling = .true.
                    end if

                elseif (bdt == "open") then
                
                else
                    print *, "Invalid boundary type"
                    exit MainLoop
                end if

                ! Topling
                if (topling) then
                    topling = .false.
                    M(x,y) = M(x,y) - limit
                    frame_size = frame_size + 1
                    F(x,y) = 1
                    A(x,y) = A(x,y) + 1

                    if (randomness) then ! Random distribution case
                    ! 1 2 3
                    ! 8 9 4
                    ! 7 6 5
                        do k = 1, limit
                            call random_number(r_randn)
                            i_randn = int(r_randn*9+1)
                            select case (i_randn)
                            case (1)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (2)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y-1

                            case (3)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y-1

                            case (4)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y

                            case (5)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x+1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (6)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y+1

                            case (7)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y+1

                            case (8)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x-1
                                nf_Queue(2, nf_queue_length) = y

                            case (9)
                                nf_queue_length = nf_queue_length +1
                                nf_Queue(1, nf_queue_length) = x
                                nf_Queue(2, nf_queue_length) = y

                            end select
                        end do

                    else ! Ordered distribution case
                        if (limit == 4) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y
                        
                        elseif (limit == 5) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y

                        elseif (limit == 8) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                        elseif (limit == 9) then
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y
                        end if
                    end if

                end if
            end do InFrameLoop

            if (nf_queue_length == 0) then
                next_sand = .true.
            end if

            ! Printing the current completation percentage
            write (*,30) real(j)*100/real(frames),"% done"

            ! Writing matrix on file
            writerLoop: do y = 1, (l-1)
                do x = 1, l
                    write(1,10, advance="no") M(x, y), ","
                    write(2,10, advance="no") F(x, y), ","
                    write(3,10, advance="no") A(x, y), ","
                end do
            end do writerLoop
            do x = 1, (l-1)
                write(1,10, advance="no") M(x, l), ","
                write(2,10, advance="no") F(x, l), ","
                write(3,10, advance="no") A(x, l), ","
            end do
            write(1,20) M(l, l)
            write(2,20) F(l, l)
            write(3,20) A(l, l)

        end do EachFrameLoop

    end do MainLoop

    close(1)
    close(2)
    close(3)

end subroutine genanimdata





subroutine prechargedSimulation (l, limit, amount, bdt, randomness, filename, maxframes)
    implicit none

    ! Input arguments
    integer, intent(in) :: l, limit, amount, maxframes
    logical, intent(in) :: randomness
    character(len=6), intent(in) :: bdt, filename

    ! Internal Variables
    integer :: j, i, k, i_randy, i_randx, queue_length, nf_queue_length, time, x, y, i_randn, frame_size
    integer :: lost_sand, sand_in_model !Enable to check if sand-processes are missing
    real :: r_randy, r_randx, r_randn
    logical :: next_sand, topling
    integer :: M(l, l), Queue(2,l*l*100), nf_Queue(2,l*l*100)

    ! Variable inicialization
    M = 0 ! Matrix where the simulation will be done
    time = 0 ! Every frame of the simulation
    nf_Queue = 0 ! Starting Queue as 0
    lost_sand = 0 ! Count lost sand in open boundaries
    

    !Formats
    20 format (I10, a)
    30 format (I10,a,I10,a,I10,a,I10)

    ! Files opening
    open (1, file = "temp/FrameData"//filename//".dat", status = 'new')

    print *, "Charging the system"

    ! Main loop (j counts the total sand added)
    ChargingLoop: do j = 1, amount

        ! Generating random numbers for adding each sand
        call random_number(r_randx)
        call random_number(r_randy)
        i_randx = int(r_randx*l+1)
        i_randy = int(r_randy*l+1)

        ! Adding first random sand to the queue
        nf_Queue(1,j) = i_randx
        nf_Queue(2,j) = i_randy

    end do ChargingLoop

    nf_queue_length = amount
    print *, "System charged succesfully"
    
    !Setting control variables to 0
    next_sand = .false. ! Control var to exit the loop when te queue is empty
    topling = .false. ! Control var to to tell if the topling should be computed


    ! Loop for processing queue, every step is a frame
    EachFrameLoop: do 
        if (time == maxframes) EXIT

        ! Manage internal variables
        time = time + 1 ! Time increase each frame
        frame_size = 0 ! Number of toplings in a frame
        Queue = nf_Queue
        queue_length = nf_queue_length
        nf_queue_length = 0

        if (queue_length > l*l*100) then
            print *, "Overloaded queue, stopping simulation"
            exit EachFrameLoop
        end if

        InFrameLoop: do i = 1, queue_length ! Go through the previous frame queue
            ! Getting coords from queue
            x = Queue(1,i)
            y = Queue(2,i)

            ! Checking boundaries
            if (x > 0 .and. x < l+1 .and. y > 0 .and. y < l+1) then ! Normal case
                M(x,y) = M(x,y) + 1
                if (M(x,y) == limit) then
                    topling = .true.
                end if

            elseif (bdt == "closed") then ! Closed boundaries case
                if (x < 1) then
                    x = 1
                elseif (x > l) then
                    x = l
                end if

                if (y < 1) then
                    y = 1
                elseif (y > l) then
                    y = l
                end if

                M(x,y) = M(x,y) + 1
                if (M(x,y) == limit) then
                    topling = .true.
                end if

            elseif (bdt == "ciclic") then ! Ciclic boundaries case
                if (x < 1) then
                    x = l
                elseif (x > l) then
                    x = 1
                end if

                if (y < 1) then
                    y = l
                elseif (y > l) then
                    y = 1
                end if

                M(x,y) = M(x,y) + 1
                if (M(x,y) == limit) then
                    topling = .true.
                end if

            elseif (bdt == "open") then
                lost_sand = lost_sand + 1
            
            else
                print *, "Invalid boundary type"
                exit InFrameLoop
            end if

            ! Topling
            if (topling) then
                topling = .false.
                M(x,y) = M(x,y) - limit
                frame_size = frame_size + 1

                if (randomness) then ! Random distribution case
                ! 1 2 3
                ! 8 9 4
                ! 7 6 5
                    do k = 1, limit
                        call random_number(r_randn)
                        i_randn = int(r_randn*9+1)
                        select case (i_randn)
                        case (1)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y-1

                        case (2)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y-1

                        case (3)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y-1

                        case (4)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y

                        case (5)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x+1
                            nf_Queue(2, nf_queue_length) = y+1

                        case (6)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y+1

                        case (7)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y+1

                        case (8)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x-1
                            nf_Queue(2, nf_queue_length) = y

                        case (9)
                            nf_queue_length = nf_queue_length +1
                            nf_Queue(1, nf_queue_length) = x
                            nf_Queue(2, nf_queue_length) = y

                        end select
                    end do

                else ! Ordered distribution case
                    if (limit == 4) then
                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y
                    
                    elseif (limit == 5) then
                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y

                    elseif (limit == 8) then
                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y

                    elseif (limit == 9) then
                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y-1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x+1
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y+1

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x-1
                        nf_Queue(2, nf_queue_length) = y

                        nf_queue_length = nf_queue_length +1
                        nf_Queue(1, nf_queue_length) = x
                        nf_Queue(2, nf_queue_length) = y
                    end if
                end if

            end if

        end do InFrameLoop
        if (nf_queue_length == 0) then
            next_sand = .true.
        end if

        sand_in_model = amount - lost_sand

        ! Writing data of the frame
        write(1,30) time, ",", frame_size, ",", lost_sand, ",", sand_in_model

        ! Printing total frames generated
    write (*,20) time," frames"
    end do EachFrameLoop

    close(1)
    close(2)

end subroutine prechargedSimulation