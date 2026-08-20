program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	notc, tci, n, m, l, r: int32;
	ans: boolean;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		l := 1;
		ans := true;
		for r := 1 to n do begin
			read(a[r]);
			if ans then begin
				if a[l] <> a[r] then
					l := r
				else
					ans := r-l+1 < m;
			end;
		end;
		readln;

		if ans then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
