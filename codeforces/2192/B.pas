program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	notc, tci, n, i, x: int32;
	s: string;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		x := 0;
		for i := 1 to n do
			if s[i] = '1' then begin
				inc(x);
				a[x] := i;
			end;

		if odd(x) then begin

			x := 0;
			for i := 1 to n do
				if s[i] = '0' then begin
					inc(x);
					a[x] := i;
				end;

			if not odd(x) then x := -1;

		end;

		writeln(x);
		if x > 0 then begin
			for i := 1 to x-1 do write(a[i], ' ');
			writeln(a[x]);
		end;

	end;
end.
