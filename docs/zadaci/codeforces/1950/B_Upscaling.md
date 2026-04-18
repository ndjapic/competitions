# Задатак: B_Upscaling.pas

```pascal
program B_Upscaling;
var
    ntc, tci, n, i, j: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 0 to 2*n-1 do begin
            for j := 0 to 2*n-1 do
                if odd(i div 2 + j div 2) then
                    write('.')
                else
                    write('#');
            writeln;
        end;

    end;
end.

```
