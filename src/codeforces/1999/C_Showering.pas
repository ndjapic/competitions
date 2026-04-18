program C_Showering;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
	nn = 200 * 1000;
var
    ntc, tci: int16;
    n, s, m, i: int32;
    ans: boolean;
    l, r: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, s, m);

		for i := 1 to n do readln(l[i], r[i]);

		ans := (l[1] >= s) or (m - r[n] >= s);
		i := 1;
		while not ans and(i < n) do begin
			ans := l[i+1] - r[i] >= s;
			inc(i);
		end;

		if ans then
			writeln('YES')
		else
			writeln('NO');

    end;
end.
