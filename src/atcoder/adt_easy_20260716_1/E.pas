program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections;
const
	NN = 10;
	MM = 200 * 1000;
var
	n, i, ai, bi: int8;
	m, j: int32;
	ch: char;
	ans: boolean;
	a, b: array [1 .. NN] of int8;
	s: array [1 .. MM] of string;
	seen: array [1 .. NN, 1 .. NN, 'a' .. 'z'] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do readln(a[i], b[i]);

	for ai := 1 to NN do
		for bi := 1 to NN do
			for ch := 'a' to 'z' do seen[ai, bi, ch] := false;

	readln(m);
	for j := 1 to m do begin
		readln(s[j]);
		ai := length(s[j]);
		for bi := 1 to ai do seen[ai, bi, s[j][bi]] := true;
	end;

	for j := 1 to m do begin
		ans := length(s[j]) = n;

		for i := 1 to n do
			ans := ans and seen[a[i], b[i], s[j][i]];

		if ans then
			writeln('Yes')
		else
			writeln('No');
	end;
end.
