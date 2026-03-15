using Microsoft.Win32;
using System.Diagnostics;
using System.Drawing;
using System.Reflection.Metadata;
using xlang.Compiler.Utils;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace xlang.Compiler.CodeGeneration;

public record TempData
{
    public required ValKind Kind { get; init; }
    public required Operand Op { get; init; }
    public required TempHandle[] OwnedHandles { get; init; }
    public bool IsFloat { get; init; }
}

public class TempHandle
{
    public TempData Data { get; set; }


    public ValKind Kind => Data.Kind;
    public Operand Op => Data.Op;
    //public int Size => Data.Size;
    public bool IsFloat => Data.IsFloat;
    public RegisterOperand Reg => (Op as RegisterOperand)!;
    public MemoryOperand Mem => (Op as MemoryOperand)!;
    public bool IsRegister => Kind is ValKind.Gpr or ValKind.Xmm;
    public TempHandle[] OwnedHandles => Data.OwnedHandles;



    public static TempHandle Imm(int val, int size) => new(ValKind.Imm, new ImmediateOperand(val, size), size, false, []);
    public static TempHandle StableMem(MemoryOperand mem, int size, bool isFloat) => new(ValKind.Mem, mem, size, isFloat, []);
    public static TempHandle StableMem(TempHandle baseReg, int displacement, int size, bool isFloat)
    {
        var memOp = MemoryOperand.FromDisplacement(baseReg.Reg.Register, displacement, size);
        return new TempHandle(ValKind.Mem, memOp, size, isFloat, []);
    }
    public static TempHandle OwnedMem(TempHandle baseReg, int displacement, int size, bool isFloat)
    {
        var memOp = MemoryOperand.FromDisplacement(baseReg.Reg.Register, displacement, size);

        return new TempHandle(ValKind.Mem, memOp, size, isFloat, [baseReg]);
    }
    public static TempHandle OwnedMem(TempHandle baseReg, TempHandle indexReg, int scale, int displacement, int size, bool isFloat)
    {
        var memOp = MemoryOperand.FromIndex(baseReg.Reg.Register, indexReg.Reg.Register, scale, displacement, size);

        return new TempHandle(ValKind.Mem, memOp, size, isFloat, [baseReg, indexReg]);
    }
    public static TempHandle Gpr(RegisterOperand r, int size) => new(ValKind.Gpr, r, size, false, []);
    public static TempHandle Xmm(RegisterOperand r, int size) => new(ValKind.Xmm, r, size, true, []);

    public static TempHandle Any(Operand r, int size, bool isFloat)
    {
        var kind = r switch
        {
            RegisterOperand { IsXmm: false } => ValKind.Gpr,
            RegisterOperand { IsXmm: true } => ValKind.Xmm,
            MemoryOperand => ValKind.Mem,
            _ => throw new ArgumentOutOfRangeException(nameof(r), r, null)
        };

        return new TempHandle(kind, r, size, isFloat, []);
    }

    private TempHandle(ValKind k, Operand op, int size, bool f, TempHandle[] owned)
    {
        Data = new TempData
        {
            Kind = k,
            Op = op,
            //Size = size,
            IsFloat = f,
            OwnedHandles = owned
        };
    }
}
public class RegisterState
{
    public bool IsUsed { get; set; }
    public TempHandle? OwnerHandle { get; set; }
}

//TODO: Rename to TempPool
public class TemporaryPool
{
    private readonly Register[] _gprs = [Register.Rax, Register.Rcx, Register.Rdx, Register.R8, Register.R9, Register.R10, Register.R11];
    private readonly LinkedList<Register> _regQueue;
    private readonly Dictionary<Register, RegisterState> _state;

    //public readonly List<TempHandle> _handles = [];

    private Action<Operand, Operand> _move;
    private Action<Operand, Operand> _exchange;

    public TemporaryPool(Action<Operand, Operand> move, Action<Operand, Operand> exchange)
    {
        _move = move;
        _regQueue = new LinkedList<Register>(_gprs);
        _state = new Dictionary<Register, RegisterState>(_gprs.Select(x => new KeyValuePair<Register, RegisterState>(x, new RegisterState())));
        _exchange = exchange;
    }

    public int FreeCount => _state.Count(x => !x.Value.IsUsed);

    public TempHandle AcquireGpr(int size = 8)
    {
        Register? reg;
        if (_state.Any(x => !x.Value.IsUsed))
        {
            reg = _state.First(x => !x.Value.IsUsed).Key;
        }
        else
        {
            throw new NotImplementedException();
        }

        var handle = TempHandle.Gpr(new RegisterOperand(reg, size), size);
        _state[reg].IsUsed = true;
        _state[reg].OwnerHandle = handle;

        return handle;
    }
    public TempHandle AcquireXmm(int size = 8) => throw new NotImplementedException();
    public TempHandle AcquireRegister(bool isFloat, int size = 8) => isFloat ? AcquireXmm(size) : AcquireGpr(size);

    public TempHandle AcquireParameter(int index, bool isFloat)
    {
        var reg = AssemblyUtils.GetParameterRegister(index);
        Debug.Assert(!_state[reg].IsUsed);

        var handle = TempHandle.Any(new RegisterOperand(reg, 8), 8, reg.IsXmm);
        _state[reg].IsUsed = true;
        _state[reg].OwnerHandle = handle;

        return handle;
    }
    public TempHandle AcquireReturn(bool isFloat)
    {
        var reg = isFloat ? Register.Xmm0: Register.Rax;
        Debug.Assert(!_state[Register.Rax].IsUsed);

        var handle = TempHandle.Any(new RegisterOperand(reg, 8), 8, reg.IsXmm);
        _state[reg].IsUsed = true;
        _state[reg].OwnerHandle = handle;

        return handle;
    }

    public void Pin(TempHandle handle)
    {
        //throw new NotImplementedException();
    }
    public void Unpin(TempHandle handle)
    {
        //throw new NotImplementedException();
    }

    public void Touch(TempHandle handle) => throw new NotImplementedException();

    public void EnsureRegister(TempHandle handle)
    {
        if (handle.IsRegister) return;

        Register? reg;
        if (_state.Any(x => !x.Value.IsUsed))
        {
            reg = _state.First(x => !x.Value.IsUsed).Key;
        }
        else
        {
            throw new NotImplementedException();
        }

        _state[reg].IsUsed = true;
        _state[reg].OwnerHandle = handle;
        _move(new RegisterOperand(reg, 8), handle.Op);
        foreach (var ownedHandle in handle.OwnedHandles)
        {
            Release(ownedHandle);
        }

        handle.Data = new TempData
        {
            Kind = handle.IsFloat ? ValKind.Xmm : ValKind.Gpr,
            Op = new RegisterOperand(reg, 8),
            //Size = handle.Size,
            IsFloat = handle.IsFloat,
            OwnedHandles = []
        };
    }
    public void EnsureRegisterAndPin(TempHandle handle)
    {
        EnsureRegister(handle);
        Pin(handle);
    }



    public void EnsureSpecificRegister(TempHandle handle, Register register)
    {
        if (handle.IsRegister && handle.Reg.Register == register) return;

        if (!_state[register].IsUsed)
        {
            _state[register].IsUsed = true;
            _state[register].OwnerHandle = handle;
            _move(new RegisterOperand(register, 8), handle.Op);
            foreach (var ownedHandle in handle.OwnedHandles)
            {
                Release(ownedHandle);
            }
            handle.Data = new TempData
            {
                Kind = handle.IsFloat ? ValKind.Xmm : ValKind.Gpr,
                Op = new RegisterOperand(register, 8),
                //Size = handle.Size,
                //IsFloat = handle.IsFloat,
                OwnedHandles = []
            };
        }
        else if (handle.IsRegister)
        {
            var other = _state[register].OwnerHandle!;
            SwapHandles(handle, other);
        }
        else
        {
            throw new NotImplementedException();
        }
    }

    public void EnsureNotMemory(TempHandle handle)
    {
        if (handle.Kind == ValKind.Mem) EnsureRegister(handle);
    }

    public void UseParameters(int count) => throw new NotImplementedException();

    public void Release(TempHandle handle)
    {
        if (handle.IsRegister)
        {

            _state[handle.Reg.Register].IsUsed = false;
            _state[handle.Reg.Register].OwnerHandle = null;
        }

        //throw new NotImplementedException();
    }

    public void Reset()
    {
        foreach (var state in _state.Values)
        {
            state.IsUsed = false;
            state.OwnerHandle = null;
        }
        //_handles.Clear();
    }

    public void SpillAll()
    {
        foreach (var state in _state.Values)
        {
            if (state.IsUsed)
            {

            }
        }
    }


    private void SpillAndAcquire()
    {
        var reg = _regQueue.First!.Value;
        _regQueue.AddLast(reg);

        //_state[reg].OwnerHandle.Spill();
    }


    private void SpillHandle(TempHandle handle)
    {
        Debug.Assert(handle.IsRegister);


    }

    private void SwapHandles(TempHandle first, TempHandle second)
    {
        Debug.Assert(first.IsRegister && second.IsRegister);
        _exchange(first.Reg, second.Reg);

        var firstOp = first.Op;
        var secondOp = second.Op;

        first.Data = first.Data with { Op = secondOp };
        second.Data = second.Data with { Op = firstOp };
    }
}
