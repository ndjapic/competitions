program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
var
	n: int32;
	ch: char;
	s: TList<char>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	n := 0;
	s := TList<char>.Create;
	while not eoln do begin
		read(ch);
		s.Add(ch);
		s.Exchange(n, Random(n+1));
		inc(n);
	end;
	readln;
	s.Sort;

	for ch in s do write(ch);
	writeln;
	s.Free;
end.
