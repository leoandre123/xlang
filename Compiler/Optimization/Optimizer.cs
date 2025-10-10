using xlang.Compiler.Structures.AST;

namespace xlang.Compiler.Optimization;



public class AstOptimizer()
{

    public List<RewriteOptimizer> Optimizers { get; } =
    [
        new ConstantFolder(),
    ];


    public ProgramDeclaration Optimize(ProgramDeclaration programDeclaration)
    {
        foreach (var op in Optimizers)
        {
            programDeclaration = op.RewriteProgramDeclaration(programDeclaration);
        }

        return programDeclaration;
    }

}


public abstract class RewriteOptimizer : NodeRewriter
{

}


public class ConstantFolder : RewriteOptimizer
{
    protected override Expression RewriteBinary(BinaryExpression binaryExpr)
    {
        var e = (BinaryExpression)base.RewriteBinary(binaryExpr);

        var lhs = e.Left;
        var rhs = e.Right;


        if (lhs is IntegerLiteralExpression li && rhs is IntegerLiteralExpression ri)
        {
            var l = li.Value;
            var r = ri.Value;
            var span = SourceSpan.FromStartAndEnd(lhs.Span.Start, rhs.Span.End);


            return e.Operator.Type switch
            {
                OperatorType.Add => new IntegerLiteralExpression(l + r, span),
                OperatorType.Sub => new IntegerLiteralExpression(l - r, span),
                OperatorType.Mul => new IntegerLiteralExpression(l * r, span),
                OperatorType.Div => new IntegerLiteralExpression(l / r, span),
                OperatorType.Mod => new IntegerLiteralExpression(l % r, span),

                OperatorType.Less => new BooleanLiteralExpression(l < r, span),
                OperatorType.LessOrEqual => new BooleanLiteralExpression(l <= r, span),
                OperatorType.Greater => new BooleanLiteralExpression(l > r, span),
                OperatorType.GreaterOrEqual => new BooleanLiteralExpression(l >= r, span),

                OperatorType.BitwiseAnd => new IntegerLiteralExpression(l & r, span),
                OperatorType.BitwiseOr => new IntegerLiteralExpression(l | r, span),
                OperatorType.BitwiseXor => new IntegerLiteralExpression(l ^ r, span),
                OperatorType.ShiftLeft => new IntegerLiteralExpression(l << r, span),
                OperatorType.ShiftRight => new IntegerLiteralExpression(l >> r, span),

                OperatorType.Equal => new BooleanLiteralExpression(l == r, span),
                OperatorType.NotEqual => new BooleanLiteralExpression(l != r, span),

                _ => e
            };
        }

        if (lhs is BooleanLiteralExpression lb && rhs is BooleanLiteralExpression rb)
        {
            var l = lb.Value;
            var r = rb.Value;
            var span = SourceSpan.FromStartAndEnd(lhs.Span.Start, rhs.Span.End);


            return e.Operator.Type switch
            {
                OperatorType.Equal => new BooleanLiteralExpression(l == r, span),
                OperatorType.NotEqual => new BooleanLiteralExpression(l != r, span),

                _ => e
            };
        }


        return e;
    }
}