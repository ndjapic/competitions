# Problem: C_The_Nether.pas

```pascal
program C_The_Nether;
{$MODE DELPHI}
uses
	Generics.Collections;
const
	nn = 500;
type
	THeapComparer<_T> = class
		function Compare(constref Left, Right: _T): SizeInt;
	end;
	THeap<_T> = class
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
	ntc, tci: int16;
	n, i: int32;
	ans: int64;
	g: array [1 .. nn] of int32;
	Comparer: THeapComparer<int32>;
	pq: THeap<int32>;

// THeapComparer<_T>
function THeapComparer<_T>.Compare(constref Left, Right: _T): SizeInt;
begin
	Result := Left - Right;
end;

// THeap<_T>
function THeap<_T>.Favorite(u: SizeInt): SizeInt;
var
	v: SizeInt;
begin
	v := u * 2 + 2;
	if (v >= Count) or (Comparer.Compare(FItems[v-1], FItems[v]) < 0) then
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

constructor THeap<_T>.Create;
begin
	Inherited Create;
	FItems := TList<_T>.Create;
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
	while (v > 0) and (Comparer.Compare(FItems[v], FItems[u]) < 0) do begin
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
	while (v < Count) and (Comparer.Compare(FItems[v], FItems[u]) < 0) do begin
		FItems.Exchange(u, v);
		u := v;
		v := Favorite(u);
	end;
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		Comparer := THeapComparer<int32>.Create;
		pq := THeap<int32>.Create;
		try

			for i := 1 to n do begin
				read(g[i]);
				pq.Push(g[i]);
			end;
			readln;

			for i := 1 to n do g[i] := pq.Pop;

			ans := 0;
			for i := 1 to n div 2 do inc(ans, g[2*i]);
			if odd(n) then inc(ans, g[n]);
			writeln(ans);

		finally
			pq.Free;
			Comparer.Free;
		end;

	end;
end.

```
