program A_Strong_Password_v2;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci: int16;
    n, i: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);

        n := length(s);
        setlength(s, n+1);
        s[n+1] := s[n];

        i := n;
        while (i > 1) and (s[i-1] <> s[i]) do begin
            s[i] := s[i-1];
            dec(i);
        end;

        if s[i+1] = 'a' then
            s[i] := 'b'
        else
            s[i] := 'a';

        writeln(s);

    end;
end.
