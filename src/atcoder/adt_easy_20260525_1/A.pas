program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	if n >= 42 then inc(n);

	s := 'AGC000';
	i := length(s);

	while n > 0 do begin
		s[i] := chr(ord('0') + n mod 10);
		n := n div 10;
		dec(i);
	end;

	writeln(s);
end.
