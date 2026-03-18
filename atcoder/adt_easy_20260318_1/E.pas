program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Classes;
const
	nn = 50 * 1000;
var
	n, i: int32;
	j: int8;
	ch: char;
	x, si: string;
	a, b: array ['a' .. 'z'] of char;
	s: TStringList;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	ch := 'a';
	for j := 1 to 26 do begin
		a[x[j]] := ch;
		b[ch] := x[j];
		ch := succ(ch);
	end;
	readln(n);

	s := TStringList.Create;
	try

		for i := 0 to n-1 do begin
			readln(si);
			for j := 1 to length(si) do si[j] := a[si[j]];
			s.Add(si);
		end;

		s.Sort;

		for i := 0 to n-1 do begin
			si := s[i];
			for j := 1 to length(si) do si[j] := b[si[j]];
			writeln(si);
		end;

	finally
		s.Free;
	end;
end.
