# Unit
C++-Based CLI which converts units.
# Target
```shell
> 4 / 3
1.(3)
> 2 + 2 * 2
6
> 0x32
50
> 23C -> F
73.4F
> 1.5m3 -> cm3
1500000
> 2 as bin
10b
> 2(2+2)
8
> |-4|
4
> x + 11b = 0xA
x = 7
> (x + 2y)(1/2y - 5^2)
```
# Get Started
```shell
git clone https://github.com/Moderrek/unit
cd unit
cmake -S . -B build
cd build
make
./Unit
```