//
//  Search.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/30/22.
//

import UIKit

enum Priority {
    case low
    case medium
    case high
    
    var toInt: Int {
        switch self {
        case .low: return 1
        case .medium: return 2
        case .high: return 3
        }
    }
    
    func dec() -> Priority {
        switch self {
        case .low: return .low
        case .medium: return .low
        case .high: return .medium
        }
    }
}

struct Index {
    let value: String
    let priority: Priority
    
    func decPriority() -> Index {
        return Index(value: value, priority: priority.dec())
    }
}

protocol Indexable {
    func index() -> [Index]
    func identifier() -> UUID
}

extension UIViewController {
    private func score(item: Indexable, token: String) -> Int {
        return item.index().compactMap({ $0.value.lowercased().contains(token) ? $0.priority.toInt : nil }).reduce(0, +)
    }
    
    private func scores(item: Indexable, tokens: [String]) -> Int {
        return tokens.map({ score(item: item, token: $0) }).reduce(0, +)
    }
    
    func search(items: [Indexable], tokens: [String]) -> [UUID] {
        return items.map({ (scores(item: $0, tokens: tokens), $0.identifier()) }).filter({ $0.0 > 0 }).sorted(by: { $0.0 < $1.0 }).reversed().map({ $0.1 })
    }
}
