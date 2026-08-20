program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
var
	i: int32;
	s: string;
	a: TList<char>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	a := TList<char>.Create;

	for i := 1 to 3 do begin
		readln(s);
		a.Add(s[2]);
		a.Exchange(i-1, Random(i));
	end;
	a.Sort;

	if a[0] <> 'B' then
		writeln('ABC')
	else if a[1] <> 'G' then
		writeln('AGC')
	else if a[2] <> 'H' then
		writeln('AHC')
	else
		writeln('ARC');

	a.Free;
end.
