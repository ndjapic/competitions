# Problem: B_Shifts_and_Sorting.pas

```pascal
program B_Shifts_and_Sorting;
{$H+}
var
    ntc, tci: int16;
    n, i, j: int32;
    ans: int64;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        j := 1;
        ans := 0;
        for i := 1 to n do
            if s[i] = '0' then begin
                if j < i then inc(ans, i-j+1);
                inc(j);
            end;

        writeln(ans);

    end;
end.

```
