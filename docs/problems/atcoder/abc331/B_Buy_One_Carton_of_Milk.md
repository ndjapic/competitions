# Problem: B_Buy_One_Carton_of_Milk.pas

```pascal
program B_Buy_One_Carton_of_Milk;
uses
    math;
const
    maxn = 100;
var
    n, s, m, l, x, y, z, money: int32;

begin
    readln(n, s, m, l);

    money := high(int32);
    for x := 0 to (n+5) div 6 do
        for y := 0 to (n+7) div 8 do
            for z := 0 to (n+11) div 12 do
                if 6*x + 8*y + 12*z >= n then
                    money := min(money, s*x + m*y + l*z);

    writeln(money);
end.

```
