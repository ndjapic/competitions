# Problem: B_Erase_First_or_Second_Letter.pas

```pascal
program B_Erase_First_or_Second_Letter;
{$H+}
var
    ntc, tci: int16;
    n, i, ans: int32;
    t: int8;
    ch: char;
    s: string;
    seen: array ['a' .. 'z'] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        for ch := 'a' to 'z' do seen[ch] := false;
        t := 0;
        ans := 0;

        for i := 1 to n do begin
            if not seen[s[i]] then begin
                inc(t);
                seen[s[i]] := true;
            end;
            inc(ans, t);
        end;

        writeln(ans);

    end;
end.

```
