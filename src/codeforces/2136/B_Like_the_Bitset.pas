program B_Like_the_Bitset;
{$MODE DELPHI}
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, k, x, c1: int32;
	s: string;
	ans: boolean;
	p: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);
		readln(s);

		x := 0;

		for i := 1 to n do
			if s[i] = '1' then begin
				inc(x);
				p[i] := x;
			end;

		for i := 1 to n do
			if s[i] = '0' then begin
				inc(x);
				p[i] := x;
			end;

		ans := true;
		c1 := 0;

		for i := 1 to n do begin
			if s[i] = '1' then
				inc(c1)
			else
				c1 := 0;
			ans := ans and (c1 < k);
		end;

		if ans then begin
			writeln('YES');
			for i := 1 to n-1 do write(p[i], ' ');
			writeln(p[n]);
		end else
			writeln('NO');

	end;
end.
