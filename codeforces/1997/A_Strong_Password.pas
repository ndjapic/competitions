program A_Strong_Password;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    n, i, j: int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

		i := 2;
		while (i <= n) and (s[i-1] <> s[i]) do inc(i);

		setlength(s, n+1);
		for j := n downto i do s[j+1] := s[j];

		if s[i-1] = 'a' then
			s[i] := 'b'
		else
			s[i] := 'a';

        writeln(s);

    end;
end.
