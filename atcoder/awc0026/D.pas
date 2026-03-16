program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
type
	TEvent = record
		x: int32;
		d: int8;
	end;
var
	n, i, k, h, x, ans: int32;
	lel, rel, elm: TEvent;
	evt: TList<TEvent>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareEvents(constref Left, Right: TEvent): Integer;
begin
	Result := Left.x - Right.x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	evt := TList<TEvent>.Create;
	lel.d := 1;
	rel.d := -1;
	for i := 1 to n do begin
		readln(lel.x, rel.x);
		evt.Add(lel);
		evt.Add(rel);
	end;
	evt.Sort(TComparer<TEvent>.Construct(CompareEvents));

	ans := 0;
	h := 0;
	x := 0;
	for elm in evt do begin
		if h >= k then inc(ans, elm.x - x);
		x := elm.x;
		inc(h, elm.d);
	end;
	writeln(ans);

	evt.Free;
end.
