# 💉 VaxProof - Digital Vaccination Verification System

## 🎯 Overview
VaxProof is a blockchain-based digital vaccination verification system that enables authorized medical institutions to issue tamper-proof vaccination certificates as NFTs.

## ✨ Features
- 🏥 Authorized issuer management
- 💳 NFT-based vaccination certificates
- ✅ On-chain verification
- ⏰ Time-based validity
- 🔄 Transferable proof of vaccination

## 🛠 Technical Implementation
The smart contract implements:
- Non-fungible token standard for vaccination proofs
- Issuer authorization system
- Secure verification mechanism
- Certificate expiration handling

## 📝 Usage

### For Contract Owner
1. Register authorized issuers:
```clarity
(contract-call? .vax-proof register-issuer <issuer-address> <name> <license-number> <country>)
```

### For Authorized Issuers
1. Mint vaccination proof:
```clarity
(contract-call? .vax-proof mint-vax-proof <patient-address> <vaccine-type> <batch-number> <validity-period> <verification-hash>)
```

### For Verifiers
1. Verify vaccination proof:
```clarity
(contract-call? .vax-proof verify-vax-proof <token-id> <proof>)
```

## 🔐 Security
- Only authorized issuers can mint vaccination proofs
- Verification uses cryptographic hashing
- Built-in expiration mechanism
- Revocation capability for issuers

## 📄 License
MIT License


