program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #list #sort #heap
uses
	Generics.Defaults, Generics.Collections, Math;
type
	TGuard = record
		l, r: int32;
	end;
	THeapComparer<_T> = class
		class function Compare(constref Left, Right: _T): SizeInt;
	end;
	THeap<_T, _C> = class
	private
		FItems: TList<_T>;
		function Favorite(u: SizeInt): SizeInt;
		function GetItem(Index: SizeInt): _T;
		function GetCount: SizeInt;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Push(Item: _T);
		function Pop: _T;
		property Count: SizeInt read GetCount;
		property Items[Index: SizeInt]: _T read GetItem; default;
	end;

var
	n, m, i, c, l, r: int32;
	loop: boolean;
	g: TGuard;
	guards: TList<TGuard>;
	pq: THeap<int32, THeapComparer<int32>>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareGuards(constref Left, Right: TGuard): Integer;
begin
	Result := Sign(Left.l - Right.l);
	if Result = 0 then Result := Sign(Left.r - Right.r);
end;

// THeapComparer<_T>
class function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := - Left + Right;
end;

// THeap<_T, _C>
function THeap<_T, _C>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (_C.Compare(FItems[v-1], FItems[v]) < 0) then
		dec(v);
	Result := v;
end;

function THeap<_T, _C>.GetItem(Index: SizeInt): _T;
begin
	Result := FItems[Index];
end;

function THeap<_T, _C>.GetCount: SizeInt;
begin
	Result := FItems.Count;
end;

constructor THeap<_T, _C>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
	{FItems.OwnsObjects := False;}
end;

destructor THeap<_T, _C>.Destroy;
begin
	FItems.Free;
	inherited Destroy;
end;

procedure THeap<_T, _C>.Push(Item: _T);
var
	u, v: SizeInt;
begin
	v := Count;
	FItems.Add(Item);
	u := (v-1) div 2;
	while (v > 0) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		v := u;
		u := (v-1) div 2;
	end;
end;

function THeap<_T, _C>.Pop: _T;
var
	u, v: SizeInt;
begin
	Result := FItems[0];
	FItems[0] := FItems[Count - 1];
	// If the Item itself needs to be freed, free it before deleting from the list
	FItems.Delete(Count - 1);

	u := 0;
	v := Favorite(u);
	while (v < Count) and (_C.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	guards := TList<TGuard>.Create;
	for i := 0 to m-1 do begin
		ReadLn(g.l, g.r);
		guards.Add(g);
	end;

	guards.Sort(TComparer<TGuard>.Construct(CompareGuards));

	l := 1;
	i := 0;
	c := 0;
	pq := THeap<int32, THeapComparer<int32>>.Create;
	loop := True;

	while loop do
		if (i < m) and (guards[i].l <= l) then begin
			pq.Push(guards[i].r);
			inc(i);
		end else if (pq.Count > 0) then begin
			r := pq.Pop;
			if l <= r then begin
				inc(c);
				l := r+1;
			end;
		end else
			loop := False;

	if l <= n then c:= -1;
	writeln(c);

	guards.Free;
	pq.Free;
end.
