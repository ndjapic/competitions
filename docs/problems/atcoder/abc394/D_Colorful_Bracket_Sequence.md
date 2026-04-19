# Problem: D_Colorful_Bracket_Sequence.pas

```pascal
program D_Colorful_Bracket_Sequence;
{$MODE DELPHI}
const
    nn = 200 * 1000;
var
    n, l, r: int32;
    ans: boolean;
    s: string;

begin
    readln(s);
    n := length(s);

    l := 0;
    r := 0;
    ans := true;

    while (r < n) and ans do begin
        inc(r);
        if (s[r] = '(') or (s[r] = '[') or (s[r] = '<') then begin
            inc(l);
            s[l] := s[r];
        end else begin
            ans := l > 0;
            if ans then
                case s[r] of
                    ')': ans := s[l] = '(';
                    ']': ans := s[l] = '[';
                    '>': ans := s[l] = '<';
                end;
            dec(l);
        end;
    end;

    if ans and (l = 0) then
        writeln('Yes')
    else
        writeln('No');
end.

```
