program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, j, k, h, c0, c1: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		setlength(s, n);
		h := k div 2;
		c0 := h+1;
		c1 := k-h+1;

		if n-c0-c1 < 0 then
			writeln(-1)
		else begin
			i := 0;

			for j := 1 to c0 do begin
				inc(i);
				s[i] := '0';
			end;

			for j := 1 to c1 do begin
				inc(i);
				s[i] := '1';
			end;

			for j := 1 to n - c0 - c1 do begin
				inc(i);
				if odd(j) then
					s[i] := '0'
				else
					s[i] := '1';
			end;

			writeln(s);
		end;

	end;
end.
