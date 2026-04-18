# Задатак: B_Haunted_House.pas

```pascal
program B_Haunted_House;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, i, c0: int32;
    ans: int64;
    s: array [1 .. maxn] of char;
    a0: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        c0 := 0;
        for i := 1 to n do begin
            read(s[i]);
            if s[i] = '0' then begin
                inc(c0);
                a0[c0] := n-i;
            end;
        end;
        readln;

        ans := 0;
        for i := 1 to n do begin
            if i > c0 then
                write('-1 ')
            else begin
                inc(ans, a0[c0+1-i] - (i-1));
                write(ans, ' ');
            end;
        end;
        writeln;

    end;
end.

```
