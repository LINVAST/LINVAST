using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>Preserves an explicit conversion without assuming language-specific conversion rules.</summary>
    public sealed class CastExprNode : ExprNode
    {
        [JsonIgnore]
        public TypeNameNode TargetType => this.Children[0].As<TypeNameNode>();

        [JsonIgnore]
        public ExprNode Operand => this.Children[1].As<ExprNode>();

        public CastExprNode(int line, TypeNameNode targetType, ExprNode operand)
            : base(line, targetType, operand) { }

        public override string GetText() => $"({this.TargetType.GetText()})({this.Operand.GetText()})";
    }
}
