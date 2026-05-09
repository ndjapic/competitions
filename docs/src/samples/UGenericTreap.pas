unit UGenericTreap;

{$mode delphi}

interface

uses
	Generics.Defaults, Generics.Collections{, SysUtils};

type
	TTreap<T> = class
	private
		type
			PNode = ^TNode;
			TNode = record
				Key: T;
				Priority: Integer;
				Count: Integer; // Za podršku duplikatima
				Left, Right: PNode;
			end;
	private
		FRoot: PNode;
		FComparer: IComparer<T>;
		function CreateNode(const AKey: T): PNode;
		procedure DisposeNode(ANode: PNode);
		procedure Split(ANode: PNode; const AKey: T; out AL, AR: PNode);
		procedure Merge(var ANode: PNode; AL, AR: PNode);
		function FindMin(ANode: PNode): PNode;
		function FindMax(ANode: PNode): PNode;
		function Find(ANode: PNode; const AKey: T): PNode;
	public
		constructor Create(AComparer: IComparer<T>);
		destructor Destroy; override;
		procedure Add(const AKey: T);
		procedure Remove(const AKey: T);
		function GetMin: T;
		function GetMax: T;
		function IsEmpty: Boolean;
	end;

implementation

constructor TTreap<T>.Create(AComparer: IComparer<T>);
begin
	inherited Create;
	FRoot := nil;
	FComparer := AComparer;
	Randomize;
end;

destructor TTreap<T>.Destroy;
begin
	DisposeNode(FRoot);
	inherited;
end;

procedure TTreap<T>.DisposeNode(ANode: PNode);
begin
	if ANode <> nil then begin
		DisposeNode(ANode^.Left);
		DisposeNode(ANode^.Right);
		Dispose(ANode);
	end;
end;

function TTreap<T>.CreateNode(const AKey: T): PNode;
begin
	New(Result);
	Result^.Key := AKey;
	Result^.Priority := Random(MaxInt);
	Result^.Count := 1;
	Result^.Left := nil;
	Result^.Right := nil;
end;

procedure TTreap<T>.Split(ANode: PNode; const AKey: T; out AL, AR: PNode);
begin
	if ANode = nil then begin
		AL := nil;
		AR := nil;
	end else if FComparer.Compare(ANode^.Key, AKey) < 0 then begin
		Split(ANode^.Right, AKey, ANode^.Right, AR);
		AL := ANode;
	end else begin
		Split(ANode^.Left, AKey, AL, ANode^.Left);
		AR := ANode;
	end;
end;

procedure TTreap<T>.Merge(var ANode: PNode; AL, AR: PNode);
begin
	if (AL = nil) or (AR = nil) then begin
		if AL <> nil then ANode := AL else ANode := AR;
	end else if AL^.Priority > AR^.Priority then begin
		Merge(AL^.Right, AL^.Right, AR);
		ANode := AL;
	end else begin
		Merge(AR^.Left, AL, AR^.Left);
		ANode := AR;
	end;
end;

function TTreap<T>.Find(ANode: PNode; const AKey: T): PNode;
var
	Cmp: Integer;
begin
	if ANode = nil then
		Result := nil
	else begin
		Cmp := FComparer.Compare(AKey, ANode^.Key);
		if Cmp < 0 then
			Result := Find(ANode^.Left, AKey)
		else if Cmp > 0 then
			Result := Find(ANode^.Right, AKey)
		else
			Result := ANode;
	end;
end;

procedure TTreap<T>.Add(const AKey: T);
var
	L, R: PNode;
	Node: PNode;
begin
	Node := Find(FRoot, AKey);
	if Node <> nil then
		Inc(Node^.Count)
	else begin
		Split(FRoot, AKey, L, R);
		Merge(L, L, CreateNode(AKey));
		Merge(FRoot, L, R);
	end;
end;

procedure TTreap<T>.Remove(const AKey: T);

	// Lokalna helper procedura za rekurzivno brisanje čvora
	procedure InternalDelete(var N: PNode; const K: T);
	var
		Cmp: Integer;
		Old: PNode;
	begin
		if N <> nil then begin
			Cmp := FComparer.Compare(K, N^.Key);
			if Cmp < 0 then
				InternalDelete(N^.Left, K)
			else if Cmp > 0 then
				InternalDelete(N^.Right, K)
			else if N^.Count > 1 then
				Dec(N^.Count) // Samo smanji brojač ako ima duplikata
			else begin
				// Ako nema više duplikata, izbaci čvor iz stabla
				Old := N;
				Merge(N, N^.Left, N^.Right); // Spoji levo i desno podstablo
				Finalize(Old^.Key);			 // Važno za stringove/generičke tipove
				Dispose(Old);				 // Oslobodi memoriju
			end;
		end;
	end;

begin
	// Pozivamo rekurzivno brisanje od korena
	InternalDelete(FRoot, AKey);
end;

function TTreap<T>.FindMin(ANode: PNode): PNode;
begin
	Result := ANode;
	if Result <> nil then
		while Result^.Left <> nil do
			Result := Result^.Left;
end;

function TTreap<T>.FindMax(ANode: PNode): PNode;
begin
	Result := ANode;
	if Result <> nil then
		while Result^.Right <> nil do
			Result := Result^.Right;
end;

function TTreap<T>.GetMin: T;
begin
	Result := FindMin(FRoot)^.Key;
end;

function TTreap<T>.GetMax: T;
begin
	Result := FindMax(FRoot)^.Key;
end;

function TTreap<T>.IsEmpty: Boolean;
begin
	Result := FRoot = nil;
end;

end.
