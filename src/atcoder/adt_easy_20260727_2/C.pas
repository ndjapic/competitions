program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
var
	n, i, j: int32;
	s: string;
	ch: char;
	a: TList<string>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(s);
	n := length(s);

	a := TList<string>.Create;
	for i := 1 to n do begin
		a.Add(s);
		a.Exchange(i-1, Random(i));

		ch := s[1];
		for j := 2 to n do s[j-1] := s[j];
		s[n] := ch;
	end;
	a.Sort;

	writeln(a[0]);
	writeln(a[n-1]);

	a.Free;
end.
