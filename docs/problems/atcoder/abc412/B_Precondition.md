# Problem: B_Precondition.pas

```pascal
program B_Precondition;
{$MODE DELPHI}
var
    i, o: int8;
    ans: boolean;
    s, t: string;
    seen: array [0 .. 63] of boolean;

begin
    readln(s);
    readln(t);

    for o := 0 to 63 do seen[o] := false;

    for i := 1 to length(t) do begin
        o := ord(t[i]) - 64;
        seen[o] := true;
    end;

    i := 2;
    ans := true;
    while (i <= length(s)) and ans do begin
        o := ord(s[i]) - 64;
        if o < 32 then begin
            o := ord(s[i-1]) - 64;
            ans := seen[o];
        end;
        inc(i);
    end;

    if ans then
        writeln('Yes')
    else
        writeln('No');
end.

```
