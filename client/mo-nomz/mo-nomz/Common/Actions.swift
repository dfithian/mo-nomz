//
//  Actions.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/11/21.
//

import UIKit
import Foundation

class Actions {
    static func loadUser(username: String, completion: ((CreateUserResponse) -> Void)?, onError: ((Error?) -> Void)?) {
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(CreateUserRequest(username: username))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            do {
                if error == nil, let d = data {
                    let output = try JSONDecoder().decode(CreateUserResponse.self, from: d)
                    completion?(output)
                } else {
                    onError?(error)
                }
            } catch {
                print("Error loading user \(error)")
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func loadIngredients(completion: ((ListIngredientResponse) -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            do {
                if error == nil, let d = data {
                    let output = try JSONDecoder().decode(ListIngredientResponse.self, from: d)
                    completion?(output)
                } else {
                    onError?(error)
                }
            } catch {
                print("Error fetching ingredients \(error)")
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func mergeIngredients(ingredientIds: [Int], ingredient: ReadableIngredient, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(MergeIngredientRequest(ids: ingredientIds, name: ingredient.name, quantity: ingredient.quantity, unit: ingredient.unit, active: ingredient.active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func deleteIngredients(ingredientIds: [Int], completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/ingredient")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteIngredientRequest(ids: ingredientIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func loadRecipes(completion: ((ListRecipeResponse) -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue("application/json", forHTTPHeaderField: "Accept")
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            do {
                if error == nil, let d = data {
                    let output = try JSONDecoder().decode(ListRecipeResponse.self, from: d)
                    completion?(output)
                } else {
                    onError?(error)
                }
            } catch {
                print("Error fetching recipes \(error)")
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func addRecipeLink(link: String, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/link")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeLinkRequest(link: link))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func addRecipeBody(name: String, content: String, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe/body")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(ImportRecipeBodyRequest(name: name, content: content))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func updateRecipe(id: Int, active: Bool, completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "POST"
        req.httpBody = try? JSONEncoder().encode(UpdateRecipeRequest(id: id, active: active))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
    
    static func deleteRecipes(recipeIds: [Int], completion: (() -> Void)?, onError: ((Error?) -> Void)?) {
        let state = Persistence.loadState()!
        var req = URLRequest(url: URL(string: Configuration.environment.baseURL + "api/v1/user/" + String(state.userId) + "/recipe")!)
        req.addValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpMethod = "DELETE"
        req.httpBody = try? JSONEncoder().encode(DeleteRecipeRequest(ids: recipeIds))
        let task = URLSession.shared.dataTask(with: req, completionHandler: { data, resp, error -> Void in
            if error == nil {
                completion?()
            } else {
                onError?(error)
            }
        })
        task.resume()
    }
}
