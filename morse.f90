program claveMorse
implicit none
character (len=10):: morse
character, dimension(:),allocatable :: cadena
integer :: i=1,j=1
allocate(cadena(1000))
do while (j==1)
    read*,morse
    select case(morse)
        case (".-")
            cadena(i)="a"
        case ("-...")
            cadena(i)="b"
        case ("-.-.")  
            cadena(i)="c" 
        case ("-..")
            cadena(i)="d"
        case (".")
            cadena(i)="e"
        case ("..-.")
            cadena(i)="f"
        case ("--.")
            cadena(i)="g"
        case ("....")
            cadena(i)="h"
        case ("..")
            cadena(i)="i"
        case (".---")
            cadena(i)="j"
        case ("-.-")
            cadena(i)="k"
        case (".-..")
            cadena(i)="l"
        case ("--")
            cadena(i)="m"
        case ("-.")
            cadena(i)="n"
        case ("---")
            cadena(i)="o"
        case (".--.")
            cadena(i)="p"
        case ("--.-")
            cadena(i)="q"
        case (".-.")
            cadena(i)="r"
        case ("...")
            cadena(i)="s"
        case ("-")
            cadena(i)="t"
        case ("..-")
            cadena(i)="u"
        case ("...-")
            cadena(i)="v"
        case (".--")
            cadena(i)="w"
        case ("-..-")
            cadena(i)="x"
        case ("-.--")
            cadena(i)="y"
        case ("--..")
            cadena(i)="z"
        case ("------")
            cadena(i)="0"
        case (".----")
            cadena(i)="1"
        case ("..---")
            cadena(i)="2"
        case ("...--")
            cadena(i)="3"
        case ("....-")
            cadena(i)="4"
        case (".....")
            cadena(i)="5"
        case ("-....")
            cadena(i)="6"
        case ("--...")
            cadena(i)="7"
        case ("---..")
            cadena(i)="8"
        case ("----..")
            cadena(i)="9"
        case (".-.-.-")
            cadena(i)="."
        case ("--..--")
            cadena(i)=","
        case ("..--..")
            cadena(i)="'"
        case (".----.")
            cadena(i)="!"
        case ("-.-.--")
            cadena(i)="/"
        case ("-..-.")
            cadena(i)="("
        case ("-.--.")
            cadena(i)=")"
        case (".-...")
            cadena(i)="&"
        case ("---...")
            cadena(i)=":"
        case ("-.-.-.")
            cadena(i)=";"
        case ("-...-")
            cadena(i)="="
        case (".-.-.")
            cadena(i)="+"
        case ("-....-")
            cadena(i)="-"
        case ("...-..-")
            cadena(i)="$"
        case ("/")
            cadena(i)="_"
        case ("...-.-")
            j=0
        case default
            print*,"Codigo incorrecto"
            j=0
    end select
    i=i+1
end do
print *,cadena
deallocate(cadena)
end program
