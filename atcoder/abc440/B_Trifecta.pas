program B_Trifecta;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, sysutils, classes, math;
const
	nn = 32;
type
	TIntComparer = class(TComparer<int32>)
		function Compare(constref L, R: int32): Integer; override;
	end;
var
	n, i: int32;
	t: array [1 .. nn] of int32;
	p: TList<int32>;
	Comparer: TIntComparer;
	InputBuf, OutputBuf: array [1..65535] of Char;

function TIntComparer.Compare(constref L, R: int32): Integer;
begin
	Result := t[L] - t[R];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;
	p := TList<int32>.Create;
	Comparer := TIntComparer.Create;
	Comparer._AddRef;

	readln(n);

	p.Clear;
	for i := 1 to n do begin
		read(t[i]);
		p.Add(i);
		p.Exchange(i-1, random(i));
	end;
	p.Sort(Comparer);

	writeln(p[0], ' ', p[1], ' ', p[2]);
	flush(StdErr); flush(output); // DO NOT REMOVE

	FreeAndNil(p);
	Comparer._Release;
end.
