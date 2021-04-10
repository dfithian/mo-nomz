//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import Foundation

class Actions {
    static func loadUser(username: String, completion: ((CreateUserResponse) -> Void)?) {
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(CreateUserRequest(username: username))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            do {
                let output = try JSONDecoder().decode(CreateUserResponse.self, from: data!)
                completion?(output)
            } catch {
                print("Error loading user \(error)")
            }
        })
        task.resume()
    }
    
    static func loadIngredients(completion: ((ListIngredientResponse) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            do {
                let output = try JSONDecoder().decode(ListIngredientResponse.self, from: data!)
                completion?(output)
            } catch {
                print("Error fetching ingredients \(error)")
            }
        })
        task.resume()
    }
    
    static func addRecipeLink(link: String, completion: (() -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/link")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in completion?() })
        task.resume()
    }
    
    static func mergeIngredients(ingredientIds: [Int], ingredient: ReadableIngredient, completion: (() -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(MergeIngredientRequest(ids: ingredientIds, name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in completion?() })
        task.resume()
    }
    
    static func deleteIngredients(ingredientIds: [Int], completion: (() -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteIngredientRequest(ids: ingredientIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in completion?() })
        task.resume()
    }
}
