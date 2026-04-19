# Problem: B_AtCoder_Amusement_Park.pas

```pascal
program B_AtCoder_Amusement_Park;
var
    n, k, i, attractions, seats, group: int8;

begin
    readln(n, k);

    attractions := 0;
    seats := k;

    for i := 1 to n do begin
        read(group);
        if seats < group then begin
            inc(attractions);
            seats := k;
        end;
        dec(seats, group);
    end;
    readln;

    if seats < k then inc(attractions);
    writeln(attractions);
end.

```
