program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, 
	Generics.Defaults;
var
	n, m, i, di: int32;
	d: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	d := TList<int32>.Create;
	d.Add(0);

	for i := 1 to n do begin
		read(di);
		d.Add(di);
	end;
	readln;

	d.Sort();
	writeln(d[n-m]);
	d.Free;
end.
