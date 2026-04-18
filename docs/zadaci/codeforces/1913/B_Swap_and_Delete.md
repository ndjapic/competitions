# Задатак: B_Swap_and_Delete.pas

```pascal
program B_Swap_and_Delete;
{$H+}
uses
    math;
var
    ntc, tci, n, i, j: int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        i := 1;
        j := 2;

        while j <= n do begin
            while (j <= n) and ((s[j] = '2') or (s[i] = s[j])) do inc(j);

            if j <= n then begin
                s[i] := '2';
                s[j] := '2';
            end;

            while (i <= n) and (s[i] = '2') do inc(i);
            j := max(j, i+1);
        end;

        writeln(n-i+1);

    end;
end.

```
