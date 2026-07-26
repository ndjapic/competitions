program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
const
	N = 5;
var
	i, x: int8;
	a: TList<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	a := TList<int8>.Create;
	for i := 1 to N do begin
		read(x);
		a.Add(x);
		a.Exchange(i-1, Random(i));
	end;
	readln;
	a.Sort;

	if a[0] <> a[1] then
		writeln('No')
	else if a[3] <> a[4] then
		writeln('No')
	else if a[3] = a[2] then
		writeln('Yes')
	else if a[3] = a[4] then
		writeln('Yes')
	else
		writeln('No');

	a.Free;
end.
