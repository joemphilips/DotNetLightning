using System.Collections.Generic;

namespace Macaroons
{
    public class VerificationResult
    {
        public bool Success { get; protected set; }
        
        public IList<string> Messages => MessageList.AsReadOnly();

        protected List<string> MessageList { get; set; }

        public VerificationResult()
        {
            Success = true;
            MessageList = new List<string>();
        }

        public VerificationResult(string msg) : this()
        {
            AddFailure(msg);
        }

        public void AddFailure(string msg)
        {
            Success = false;
            MessageList.Add(msg);
        }

        public void MergeFailures(VerificationResult src)
        {
            Success = Success && src.Success;
            MessageList.AddRange(src.MessageList);
        }
    }
}