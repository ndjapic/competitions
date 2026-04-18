program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	notc, tci, n, i, mx, ans: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		ans := 0;
		mx := 0;
		for i := 1 to n do begin
			read(a[i]);
			if a[i] >= mx then begin
				inc(ans);
				mx := a[i];
			end;
		end;
		readln;

		writeln(ans);

	end;
end.
