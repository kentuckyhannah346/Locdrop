# 🌍 Locdrop - GPS-Based NFT Airdrops

## 📍 Overview

Locdrop is a revolutionary Clarity smart contract that enables **GPS-based NFT airdrops** using augmented reality concepts. Users can create location-specific NFT drops that can only be claimed when physically present at designated coordinates! 🎯

## ✨ Features

- 🗺️ **GPS-Based Claims**: NFTs can only be claimed when users are within specified geographic coordinates
- 📱 **Real-time Location Updates**: Users update their location to participate in drops
- ⏰ **Time-Limited Drops**: Set start and end blocks for drop availability
- 🎯 **Radius Control**: Define precise claim areas with customizable radius
- 🔒 **Oracle Integration**: Authorized oracles can update user locations for enhanced security
- 🎨 **Custom NFT Metadata**: Each drop can have unique token URIs and metadata
- 👥 **Multi-User Support**: Multiple users can claim from the same drop (up to max limit)
- ⚡ **Admin Controls**: Contract owner can pause/unpause and manage oracles

## 🚀 Getting Started

### Prerequisites
- Clarinet installed
- Stacks wallet for testing

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Deploy using Clarinet:

```bash
clarinet deploy
```

## 📖 Usage Guide

### 🎯 Creating a Drop

```clarity
(contract-call? .locdrop create-drop 
  40748817    ;; latitude (NYC coordinates * 1000000)
  -73985428   ;; longitude (NYC coordinates * 1000000) 
  u1000       ;; radius in coordinate units
  "https://example.com/nft-metadata.json"  ;; token URI
  u100        ;; reward amount
  u1000       ;; duration in blocks
  u50)        ;; max claims allowed
```

### 📍 Updating Your Location

```clarity
(contract-call? .locdrop update-location 40748817 -73985428)
```

### 🎁 Claiming a Drop

```clarity
(contract-call? .locdrop claim-drop u1)  ;; drop-id
```

### 🔍 Checking Drop Information

```clarity
(contract-call? .locdrop get-drop u1)
```

### 📱 Verifying Claim Eligibility

```clarity
(contract-call? .locdrop is-drop-claimable u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🗺️ Coordinate System

- **Latitude**: Range from -90,000,000 to 90,000,000 (degrees × 1,000,000)
- **Longitude**: Range from -180,000,000 to 180,000,000 (degrees × 1,000,000)
- **Radius**: Distance units in coordinate system

### Example Coordinates:
- 🗽 New York City: `40748817, -73985428`
- 🌉 San Francisco: `37774929, -122419416`
- 🗼 Paris: `48858844, 2294351`


