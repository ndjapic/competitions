# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TAdventurer = record
		a, b: int32;
	end;
	THeap<_T> = class
	private
		FItems: TList<_T>;
		FComparer: IComparer<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create(const AComparer: IComparer<_T>);
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	n, k, i: int32;
	s, ans: int64;
	adv: TAdventurer;
	advs: TList<TAdventurer>;
	pq: THeap<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareAdventurers(constref Left, Right: TAdventurer): Integer;
begin
	Result := Right.b - Left.b;
end;

function CompareInts(constref Left, Right: int32): Integer;
begin
	Result := Left - Right;
end;

// THeap<_T> Implementation
function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (FComparer.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T>.Create(const AComparer: IComparer<_T>);
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	FComparer := AComparer;
end;

destructor THeap<_T>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (FComparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	advs := TList<TAdventurer>.Create;
	advs.Capacity := n;

	for i := 0 to n-1 do begin
		ReadLn(adv.a, adv.b);
		advs.Add(adv);
		advs.Exchange(i, random(i+1));
	end;

	advs.Sort(TComparer<TAdventurer>.Construct(CompareAdventurers));
	pq := THeap<int32>.Create(TComparer<int32>.Construct(CompareInts));

	s := 0;
	for i := 0 to k-1 do begin
		pq.Push(advs[i].a);
		inc(s, advs[i].a);
	end;

	ans := s * advs[i].b;
	for i := k to n-1 do begin
		pq.Push(advs[i].a);
		inc(s, advs[i].a - pq.Pop);
		ans := max(ans, s * advs[i].b);
	end;

	writeln(ans);
	advs.Free;
	pq.Free;
end.

```
