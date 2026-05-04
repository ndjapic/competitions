program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections,
	Generics.Defaults;
var
	n, i: int8;
	sl: TList<string>;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareBlocks(constref Left, Right: string): Integer;
begin
	Result := Length(Left) - Length(Right);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);
	sl := TList<string>.Create;

	for i := 0 to n-1 do begin
		readln(s);
		sl.Add(s);
		sl.Exchange(i, random(i+1));
	end;

	sl.Sort(TComparer<string>.Construct(CompareBlocks));

	for s in sl do write(s);
	writeln;

	sl.Free;
end.
