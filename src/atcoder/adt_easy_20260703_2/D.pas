program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 10;
var
	n, i, i0: int8;
	s, t: string;
	found: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	setlength(t, NN+2);
	for i := 1 to NN+2 do
		if i mod 3 = 1 then
			t[i] := 'o'
		else
			t[i] := 'x';

	found := false;
	for i0 := 0 to 2 do
		if not found then begin
			i := 1;
			while (i <= n) and (s[i] = t[i0 + i]) do inc(i);
			found := i > n;
		end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
