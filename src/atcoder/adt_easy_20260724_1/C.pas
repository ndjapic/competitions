program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i, j: int8;
	ans: boolean;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function getch(i, j, k: int8): char;
var
	n: int8;
begin
	n := length(s[i]);
	if k <= n then
		result := s[i][k]
	else
		result := s[j][k - n];
end;

function pal(i, j: int8): boolean;
var
	l, r: int8;
begin
	l := 1;
	r := length(s[i]) + length(s[j]);
	while (l < r) and (getch(i, j, l) = getch(i, j, r)) do begin
		inc(l);
		dec(r);
	end;
	result := l >= r;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := false;
	for i := 1 to n do begin
		readln(s[i]);
		if not ans then
			for j := 1 to i-1 do
				if not ans then
					ans := pal(i, j) or pal(j, i);
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
