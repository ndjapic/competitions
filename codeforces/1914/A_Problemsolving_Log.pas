program A_Problemsolving_Log;
{$H+}
uses
    math;
var
    ntc, tci: int8;
    n, i: int32;
    j, ans: int8;
    s: string;
    t: array [1 .. 26] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        for j := 1 to 26 do t[j] := 0;

        readln(n);
        readln(s);

        for i := 1 to n do inc(t[ord(s[i]) and 31]);

        ans := 0;
        for j := 1 to 26 do
            if t[j] >= j then inc(ans);

        writeln(ans);

    end;
end.
