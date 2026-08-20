program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 200;
var
	n, m, i, j, x: int32;
	prev: char;
	found: boolean;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	setlength(s, NN);
	for x := 1 to NN do s[x] := '.';

	for i := 1 to n do begin
		read(x);
		s[x] := 'a';
	end;
	readln;

	for j := 1 to m do begin
		read(x);
		s[x] := 'b';
	end;
	readln;

	prev := 'b';
	x := 0;
	found := false;

	while (x < NN) and not found do begin
		inc(x);
		if s[x] <> '.' then begin
			if s[x] = 'a' then found := prev = 'a';
			prev := s[x];
		end;
	end;

	if found then
		writeln('Yes')
	else
		writeln('No');
end.
