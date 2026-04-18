program D_String_Rotation;
{$MODE DELPHI}
const
    nn = 300 * 1000;
var
    ntc, tci, n, i, l, r: int32;
    s: string;
    ch: char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        l := 1;
        while (l < n) and (s[l] <= s[l+1]) do inc(l);

        if l < n then begin

            ch := s[l];
            r := l+1;
            while (r <= n) and (s[r] <= s[l]) do inc(r);
            dec(r);

            for i := l to r-1 do s[i] := s[i+1];
            s[r] := ch;

        end;

        writeln(s);

    end;
end.
