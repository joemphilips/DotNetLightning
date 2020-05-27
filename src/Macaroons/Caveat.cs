using System;
using System.Diagnostics;

namespace Macaroons
{
    [DebuggerDisplay("Caveat: {" + nameof(Inspect) + "()}")]
    public class Caveat
    {
        /// <summary>
        /// Caveat identifier.
        /// </summary>
        public Packet? CId { get; protected set; }
        
        /// <summary>
        /// Verification identifier
        /// </summary>
        public Packet? VId { get; protected set; }
        
        /// <summary>
        /// Caveat location.
        /// </summary>
        public Packet? Cl { get; protected set; }
        
        public Caveat(string cid, string cl = null)
        {
            if (cid == null) throw new ArgumentNullException(nameof(cid));
            CId = new Packet(cid);
            if (cl != null)
                Cl = new Packet(cl);
        }

        public Caveat(Packet cid, Packet vid, Packet cl)
        {
            if (cid == null) throw new ArgumentNullException(nameof(cid));
            CId = cid;
            VId = vid;
            Cl = cl;
        }

        public Caveat(Caveat src)
        {
            CId = new Packet(src.CId);
            if (src.VId != null)
                VId = new Packet(src.VId);
            if (src.Cl != null)
                Cl = new Packet(src.Cl);
        }

        public bool IsFirstPartyCaveat => VId is null;
        public bool IsTheirPartyCaveat => VId != null;

        public string Inspect()
        {
            return $"CId = {CId}" +
                (VId != null ? $"{Environment.NewLine}  VId = {VId}": "") +
                (Cl != null ? $"{Environment.NewLine}  Cl = {Cl}": "");
        }

        public override string ToString() => CId.ToString();
    }
}