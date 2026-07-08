program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, d, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	if b = 0 then
		s := '0.000'
	else if b = a then
		s := '1.000'
	else begin
		s := '0.000';
		for i := 3 to 5 do begin
			b := b * 10;
			d := b div a;
			b := b mod a;
			if (i = 5) and (a-b < b) then inc(d);
			s[i] := chr(ord('0') + d);
		end;
	end;

	writeln(s);
end.
