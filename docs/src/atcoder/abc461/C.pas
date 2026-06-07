program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
const
	NN = 200 * 1000;
type
	TGem = record
		c, v: int32;
	end;
var
	n, k, m, i, x: int32;
	ans: int64;
	gem: TGem;
	gems: TList<TGem>;
	seen, choosen: array [0 .. NN] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareGems(constref Left, Right: TGem): Integer;
begin
	Result := CompareValue(Left.v, Right.v);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n, k, m);
	gems := TList<TGem>.Create;
	gems.Capacity := n;

	for i := 0 to n-1 do begin
		ReadLn(gem.c, gem.v);
		gems.Add(gem);
		gems.Exchange(i, Random(i+1));
		choosen[i] := false;
	end;
	gems.Sort(TComparer<TGem>.Construct(CompareGems));

	for i := 1 to n do seen[i] := false;
	x := 0;

	for i := n-1 downto 0 do
		if (x < m) and not seen[gems[i].c] then begin
			seen[gems[i].c] := true;
			choosen[i] := true;
			inc(x);
		end;

	for i := n-1 downto 0 do
		if (x < k) and not choosen[i] then begin
			choosen[i] := true;
			inc(x);
		end;

	ans := 0;
	for i := 0 to n-1 do
		if choosen[i] then inc(ans, gems[i].v);
	writeln(ans);
	gems.Free;
end.
